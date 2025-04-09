package net.p3pp3rf1y.sophisticatedbackpacks.command;

import com.mojang.brigadier.arguments.BoolArgumentType;
import com.mojang.brigadier.builder.ArgumentBuilder;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;
import net.minecraft.commands.arguments.EntityArgument;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.chat.*;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.sounds.SoundEvents;
import net.minecraft.sounds.SoundSource;
import net.minecraft.world.entity.item.ItemEntity;
import net.minecraft.world.item.ItemStack;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.*;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModDataComponents;
import net.p3pp3rf1y.sophisticatedcore.util.RandHelper;

import java.util.*;

public class TemplateCommand {
	private TemplateCommand() {}

	@SuppressWarnings("java:S1452")
	static ArgumentBuilder<CommandSourceStack, ?> register() {
		return Commands.literal("template")
				.then(Commands.literal("list").executes(context -> listTemplates(context.getSource())))
				.then(Commands.literal("create")
						.then(Commands.argument("templateName", BackpackTemplateArgumentType.templateName())
								.executes(context ->	createTemplate(context.getSource(), context.getArgument("templateName", String.class), false))
								.then(Commands.argument("override", BoolArgumentType.bool())
										.executes(context ->
												createTemplate(context.getSource(), context.getArgument("templateName", String.class), BoolArgumentType.getBool(context, "override"))
										)
								)
						)
				)
				.then(Commands.literal("delete")
						.then(Commands.argument("templateName", BackpackTemplateArgumentType.templateName())
								.executes(context -> deleteTemplate(context.getSource(), context.getArgument("templateName", String.class)))
						)
				)
				.then(Commands.literal("give")
						.then(Commands.argument("templateName", BackpackTemplateArgumentType.templateName())
								.executes(context -> giveBackpackFromTemplate(context.getSource(), context.getArgument("templateName", String.class), List.of(context.getSource().getPlayer())))
								.then(Commands.argument("targets", EntityArgument.players())
										.executes(context -> giveBackpackFromTemplate(context.getSource(), context.getArgument("templateName", String.class), EntityArgument.getPlayers(context, "targets")))
								)
						)
				);
	}

	private static int listTemplates(CommandSourceStack source) {
		BackpackTemplates.getTemplateNames().forEach(templateName -> {
			source.sendSuccess(() -> Component.translatable("commands.sophisticatedbackpacks.template.list", templateName), false);
		});
		return 0;
	}

	private static int createTemplate(CommandSourceStack source, String templateName, boolean override) {
		if (!source.isPlayer()) {
			return 1;
		}

		ServerPlayer player = source.getPlayer();
		ItemStack backpack = player.getMainHandItem();
		if (!(backpack.getItem() instanceof BackpackItem backpackItem)) {
			source.sendFailure(Component.translatable("commands.sophisticatedbackpacks.template.create.nobackpack"));
			return 2;
		}

		IBackpackWrapper backpackWrapper = BackpackWrapper.fromStack(backpack);
		Optional<UUID> backpackUuid = backpackWrapper.getContentsUuid();
		if (backpackUuid.isEmpty()) {
			source.sendFailure(Component.translatable("commands.sophisticatedbackpacks.template.create.backpackempty"));
			return 3;
		}

		if (BackpackTemplates.getTemplateNames().contains(templateName) && !override) {
			source.sendFailure(Component.translatable("commands.sophisticatedbackpacks.template.create.alreadyexists", templateName));
			return 4;
		}

		BackpackTemplates.setBackpackTemplate(templateName, BuiltInRegistries.ITEM.getKey(backpackItem), BackpackStorage.get().getOrCreateBackpackContents(backpackUuid.get()).copy(), true);
		source.sendSuccess(() -> Component.translatable("commands.sophisticatedbackpacks.template.create.success", templateName), true);
		return 0;
	}

	private static int deleteTemplate(CommandSourceStack source, String templateName) {
		BackpackTemplates.removeBackpackTemplate(templateName);
		source.sendSuccess(() -> Component.translatable("commands.sophisticatedbackpacks.template.delete.success", templateName), true);
		return 0;
	}

	private static int giveBackpackFromTemplate(CommandSourceStack source, String templateName, Collection<ServerPlayer> players) {
		CompoundTag templateData = BackpackTemplates.getBackpackTemplate(templateName);
		if (templateData == null) {
			return 1;
		}

		ItemStack backpack = new ItemStack(BuiltInRegistries.ITEM.get(ResourceLocation.parse(templateData.getString("backpackItemRegistryName"))));
		backpack.set(ModDataComponents.TEMPLATE_NAME, templateName);

		players.forEach(p -> giveBackpackToPlayer(backpack.copy(), p));

		if (players.size() == 1) {
			source.sendSuccess(() -> Component.translatable("commands.sophisticatedbackpacks.template.give.backpack.success", players.iterator().next().getDisplayName()), true);
		} else {
			source.sendSuccess(() -> Component.translatable("commands.sophisticatedbackpacks.template.give.backpack.success", players.size()), true);
		}
		return 0;
	}

	private static void giveBackpackToPlayer(ItemStack backpack, ServerPlayer p) {
		boolean flag = p.getInventory().add(backpack);
		if (flag && backpack.isEmpty()) {
			backpack.setCount(1);
			ItemEntity itemEntity = p.drop(backpack, false);
			if (itemEntity != null) {
				itemEntity.makeFakeItem();
			}

			p.level().playSound(null, p.getX(), p.getY(), p.getZ(), SoundEvents.ITEM_PICKUP, SoundSource.PLAYERS, 0.2F, (RandHelper.getRandomMinusOneToOne(p.getRandom()) * 0.7F + 1.0F) * 2.0F);
			p.inventoryMenu.broadcastChanges();
		} else {
			ItemEntity itementity = p.drop(backpack, false);
			if (itementity != null) {
				itementity.setNoPickUpDelay();
				itementity.setThrower(p);
			}
		}

		ItemEntity itemEntity = p.drop(backpack, false);
		if (itemEntity != null) {
			itemEntity.makeFakeItem();
		}
	}
}
