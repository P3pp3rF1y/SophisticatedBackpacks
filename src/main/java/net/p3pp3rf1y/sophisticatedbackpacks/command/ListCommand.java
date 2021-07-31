package net.p3pp3rf1y.sophisticatedbackpacks.command;

import com.mojang.brigadier.builder.ArgumentBuilder;
import net.minecraft.command.CommandSource;
import net.minecraft.command.Commands;
import net.minecraft.util.text.Color;
import net.minecraft.util.text.IFormattableTextComponent;
import net.minecraft.util.text.StringTextComponent;
import net.minecraft.util.text.TextFormatting;
import net.minecraft.util.text.TranslationTextComponent;
import net.minecraft.util.text.event.ClickEvent;
import net.minecraft.util.text.event.HoverEvent;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.AccessLogRecord;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackAccessLogger;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.Date;
import java.util.List;

public class ListCommand {
	private ListCommand() {}

	@SuppressWarnings("java:S1452")
	static ArgumentBuilder<CommandSource, ?> register() {
		return Commands.literal("list")
				.executes(context -> printBackpackList(new ArrayList<>(BackpackAccessLogger.getAllBackpackLogs()), context.getSource()))
				.then(Commands.argument("playerName", BackpackPlayerArgumentType.playerName())
						.executes(context -> printBackpackList(new ArrayList<>(BackpackAccessLogger.getBackpackLogsForPlayer(context.getArgument("playerName", String.class))), context.getSource()))
				);
	}

	private static int printBackpackList(List<AccessLogRecord> allLogs, CommandSource source) {
		SimpleDateFormat dateFormat = new SimpleDateFormat();
		allLogs.sort(Comparator.comparing(AccessLogRecord::getAccessTime).reversed());
		source.sendSuccess(new TranslationTextComponent("commands.sophisticatedbackpacks.list.header"), false);
		allLogs.forEach(alr -> {
			IFormattableTextComponent message = new StringTextComponent("");
			message.append(new StringTextComponent(alr.getBackpackName())
					.withStyle(s ->
							s.withColor(TextFormatting.GREEN).withClickEvent(new ClickEvent(ClickEvent.Action.SUGGEST_COMMAND, "/sophisticatedbackpacks give @p " + alr.getBackpackUuid()))
									.withHoverEvent(new HoverEvent(HoverEvent.Action.SHOW_TEXT, new TranslationTextComponent("chat.sophisticatedbackpacks.backpack_uuid.tooltip", alr.getBackpackUuid())))
					)
			);
			message.append(new StringTextComponent(", "));
			TranslationTextComponent clothColor = new TranslationTextComponent("commands.sophisticatedbackpacks.list.cloth_color");
			clothColor.withStyle(clothColor.getStyle().withColor(Color.fromRgb(alr.getClothColor())));
			message.append(clothColor);
			message.append(new StringTextComponent(" "));
			TranslationTextComponent trimColor = new TranslationTextComponent("commands.sophisticatedbackpacks.list.trim_color");
			trimColor.withStyle(trimColor.getStyle().withColor(Color.fromRgb(alr.getTrimColor())));
			message.append(trimColor);
			message.append(new StringTextComponent(", "));
			message.append(new StringTextComponent(alr.getPlayerName()));
			message.append(new StringTextComponent(", "));
			message.append(new StringTextComponent(dateFormat.format(new Date(alr.getAccessTime()))));
			source.sendSuccess(message, false);
		});
		return 0;
	}
}
