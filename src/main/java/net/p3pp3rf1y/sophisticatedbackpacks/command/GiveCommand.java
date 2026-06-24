package net.p3pp3rf1y.sophisticatedbackpacks.command;

import com.mojang.brigadier.builder.ArgumentBuilder;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;
import net.minecraft.commands.arguments.EntityArgument;
import net.minecraft.core.component.DataComponents;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.network.chat.Component;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.sounds.SoundEvents;
import net.minecraft.sounds.SoundSource;
import net.minecraft.world.entity.item.ItemEntity;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackAccessLogger;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedcore.util.RandHelper;

import java.util.Collection;
import java.util.UUID;

public class GiveCommand {
	private GiveCommand() {
	}

	@SuppressWarnings("java:S1452")
	static ArgumentBuilder<CommandSourceStack, ?> register() {
		return Commands.literal("give")
				.then(Commands.argument("targets", EntityArgument.players())
						.then(Commands.argument("backpackUuid", BackpackUUIDArgumentType.backpackUuid()).executes(context -> giveBackpack(context.getSource(),
								context.getArgument("backpackUuid", UUID.class), EntityArgument.getPlayers(context, "targets")))));
	}

	private static int giveBackpack(CommandSourceStack source, UUID backpackUuid, Collection<ServerPlayer> players) {
		BackpackAccessLogger.getBackpackLog(backpackUuid).ifPresent(alr -> {
			Item item = BuiltInRegistries.ITEM.get(alr.backpackItemRegistryName()).orElseThrow().value();
			ItemStack backpack = new ItemStack(item);
			if (!backpack.getHoverName().getString().equals(alr.backpackName())) {
				backpack.set(DataComponents.CUSTOM_NAME, Component.literal(alr.backpackName()));
			}
			IBackpackWrapper backpackWrapper = BackpackWrapper.fromStack(backpack);
			backpackWrapper.setColors(alr.clothColor(), alr.trimColor());
			backpackWrapper.setColumnsTaken(alr.columnsTaken(), false);
			backpackWrapper.setContentsUuid(backpackUuid);

			players.forEach(p -> giveBackpackToPlayer(backpack, p));

			if (players.size() == 1) {
				source.sendSuccess(() -> Component.translatable("commands.sophisticatedbackpacks.give.success", players.iterator().next().getDisplayName()),
						true);
			} else {
				source.sendSuccess(() -> Component.translatable("commands.sophisticatedbackpacks.give.success", players.size()), true);
			}
		});
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

			p.level().playSound(null, p.getX(), p.getY(), p.getZ(), SoundEvents.ITEM_PICKUP, SoundSource.PLAYERS, 0.2F,
					(RandHelper.getRandomMinusOneToOne(p.getRandom()) * 0.7F + 1.0F) * 2.0F);
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
