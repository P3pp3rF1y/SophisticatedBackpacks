package net.p3pp3rf1y.sophisticatedbackpacks.command;

import com.mojang.brigadier.CommandDispatcher;
import com.mojang.brigadier.tree.LiteralCommandNode;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;
import net.minecraft.commands.synchronization.ArgumentTypeInfo;
import net.minecraft.commands.synchronization.ArgumentTypeInfos;
import net.minecraft.commands.synchronization.SingletonArgumentInfo;
import net.minecraft.core.registries.Registries;
import net.neoforged.bus.api.IEventBus;
import net.neoforged.neoforge.common.NeoForge;
import net.neoforged.neoforge.event.RegisterCommandsEvent;
import net.neoforged.neoforge.registries.DeferredRegister;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;

import java.util.function.Supplier;

public class SBPCommand {
	private static final int OP_LEVEL = 2;
	private static final DeferredRegister<ArgumentTypeInfo<?, ?>> COMMAND_ARGUMENT_TYPES = DeferredRegister.create(Registries.COMMAND_ARGUMENT_TYPE, SophisticatedBackpacks.MOD_ID);

	private static final Supplier<SingletonArgumentInfo<BackpackUUIDArgumentType>> BACKPACK_UUID_COMMAND_ARGUMENT_TYPE = COMMAND_ARGUMENT_TYPES.register("backpack_uuid", () ->
			ArgumentTypeInfos.registerByClass(BackpackUUIDArgumentType.class, SingletonArgumentInfo.contextFree(BackpackUUIDArgumentType::backpackUuid)));
	private static final Supplier<SingletonArgumentInfo<BackpackPlayerArgumentType>> PLAYER_NAME_COMMAND_ARGUMENT_TYPE = COMMAND_ARGUMENT_TYPES.register("player_name", () ->
			ArgumentTypeInfos.registerByClass(BackpackPlayerArgumentType.class, SingletonArgumentInfo.contextFree(BackpackPlayerArgumentType::playerName)));
	private static final Supplier<SingletonArgumentInfo<BackpackTemplateArgumentType>> TEMPLATE_NAME_COMMAND_ARGUMENT_TYPE = COMMAND_ARGUMENT_TYPES.register("template_name", () ->
			ArgumentTypeInfos.registerByClass(BackpackTemplateArgumentType.class, SingletonArgumentInfo.contextFree(BackpackTemplateArgumentType::templateName)));
	private static final Supplier<SingletonArgumentInfo<BackpackItemArgumentType>> BACKPACK_ITEM_ARGUMENT_TYPE = COMMAND_ARGUMENT_TYPES.register("backpack_item", () ->
			ArgumentTypeInfos.registerByClass(BackpackItemArgumentType.class, SingletonArgumentInfo.contextAware(BackpackItemArgumentType::item)));
	private static final Supplier<SingletonArgumentInfo<BackpackUpgradeItemArgumentType>> BACKPACK_UPGRADE_ITEM_ARGUMENT_TYPE = COMMAND_ARGUMENT_TYPES.register("backpack_upgrade_item", () ->
			ArgumentTypeInfos.registerByClass(BackpackUpgradeItemArgumentType.class, SingletonArgumentInfo.contextAware(BackpackUpgradeItemArgumentType::item)));

	private SBPCommand() {}

	public static void init(IEventBus modBus) {
		COMMAND_ARGUMENT_TYPES.register(modBus);

		NeoForge.EVENT_BUS.addListener(SBPCommand::registerCommands);
	}

	private static void registerCommands(RegisterCommandsEvent event) {
		CommandDispatcher<CommandSourceStack> dispatcher = event.getDispatcher();
		LiteralCommandNode<CommandSourceStack> mainNode = dispatcher.register(
				Commands.literal("sbp")
						.requires(cs -> cs.hasPermission(OP_LEVEL))
						.then(ListCommand.register())
						.then(GiveCommand.register())
						.then(RemoveNonPlayerCommand.register())
						.then(TemplateCommand.register(event))
		);
		dispatcher.register(Commands.literal("sophisticatedbackpacks").requires(cs -> cs.hasPermission(OP_LEVEL)).redirect(mainNode));
	}
}
